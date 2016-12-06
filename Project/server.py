from twisted.internet.protocol import Factory, Protocol
from twisted.protocols.basic import LineReceiver
from twisted.internet import reactor
from twisted.web.client import getPage
import sys, time, json, conf


COMMAND = ["IAMAT", "AT", "WHATSAT"]
LENGTH_OR_ARGS = {"IAMAT": 4, "AT": 6, "WHATSAT": 4, "MIN": 4}
PROP_ASSOCIATE = {
    'Alford': ['Hamilton', 'Welsh'],
    'Ball': ['Holiday', 'Welsh'],
    'Hamilton': ['Alford', 'Holiday'],
    'Holiday': ['Ball', 'Hamilton'],
    'Welsh': ['Alford', 'Ball']
}


# http://stackoverflow.com/questions/354038/how-do-i-check-if-a-string-is-a-number-float-in-python
def validFloat(s):
    try:
        float(s)
        return True
    except ValueError:
        return False


class Propagate(Protocol):
    def __init__(self, line):
        self.line = line

    def connectionMade(self):
        self.transport.write(self.line)
        self.transport.loseConnection()


class PropagateFactory(Factory):
    def startedConnecting(self, connector):
        return

    def __init__(self, line, fp):
        self.line = line + "\r\n"
        self.fp = fp

    def buildProtocol(self, addr):
        return Propagate(self.line)

    def clientConnectionLost(self, connector, reason):
        self.fp.write("Propogation connection lost. Reason: {0}\n".format(reason))

    def clientConnectionFailed(self, connector, reason):
        self.fp.write("Propogation connection failed. Error: {0}\n".format(reason))


class Server(LineReceiver):
    def __init__(self, factory):
        self.name = None
        self.factory = factory
        self.clients = factory.clients
        self.fp = factory.fp
        self.server = factory.name
        self.serverList = PROP_ASSOCIATE[self.server]

    def connectionMade(self):
        self.factory.numConnections += 1
        msg = "A new client connected. Total: {0}".format(self.factory.numConnections)
        self.sendLine(msg)
        self.fp.write(msg + "\n")

    def connectionLost(self, reason):
        self.factory.numConnections -= 1
        msg = "A client disconnected. Total: {0}. Reason is: {1}".format(
            self.factory.numConnections, reason.getErrorMessage())
        if not self.fp.closed:
            self.fp.write(msg + "\n")

    def receiveError(self, line, error):
        msg = "? " + line
        self.sendLine(msg)
        self.fp.write("Error: {0}\nServer's resonse: {1}\n".format(error, msg))

    def lineReceived(self, line):
        if not len(line):
            self.receiveError(line, "Empty line")
            return

        infoToProcess = line.strip()
        clientMsg = infoToProcess.split()
        if len(clientMsg) < LENGTH_OR_ARGS["MIN"]:
            self.receiveError(infoToProcess, "Too few arguments")
            return

        self.fp.write("Client Sent: \n" + infoToProcess + "\n")

        if clientMsg[0] == COMMAND[0]:
            self.handleIAMAT(clientMsg)
        elif clientMsg[0] == COMMAND[1]:
            self.handleAT(clientMsg)
        elif clientMsg[0] == COMMAND[2]:
            self.handleWHATSAT(clientMsg)
        else:
            self.receiveError(infoToProcess, "Unrecognized command. Please use IAMAT or WHATSAT.")

    def genATMsg(self, clientTime, infoStored):
        timeDiff = str(time.time() - float(clientTime))
        if timeDiff[0] != "-":
            timeDiff = "+" + timeDiff
        return " ".join([COMMAND[1], self.server, timeDiff, infoStored])

    def parseLocation(self, location):
        return location.replace("+", " +").replace("-", " -").split()

    def propagate(self, line, origServer, excludePropagator = True):
        for server in self.serverList:
            if excludePropagator or server != origServer:
                self.fp.write("Try to propogate to " + server + "\n")
                reactor.connectTCP("localhost", conf.PORT_NUM[server], PropagateFactory(line, self.fp))

    def handleIAMAT(self, msg):
        if len(msg) != LENGTH_OR_ARGS["IAMAT"]:
            self.receiveError(" ".join(msg), "Number of input for IAMAT is incorrect.")
            return
        self.name = msg[1]
        location = msg[2]
        if "+" not in location and "-" not in location:
            self.receiveError(" ".join(msg), "Invalid location parameter in the input.")
            return
        location = self.parseLocation(location)
        if len(location) != 2 or (not validFloat(location[0]) or not validFloat(location[1])):
            self.receiveError(" ".join(msg), "Invalid location parameter in the input.")
            return

        clientTime = msg[3]
        if not validFloat(clientTime):
            self.receiveError(" ".join(msg), "Invalid time parameter in the input.")
            return

        infoToStore = " ".join(msg[1:])
        self.clients[self.name] = infoToStore
        line = self.genATMsg(clientTime, infoToStore)
        self.sendLine(line)
        self.fp.write("Server responds:\n" + line + "\n")
        self.propagate(line, "Client", False)  # Propagate

    def handleAT(self, msg):
        if len(msg) != LENGTH_OR_ARGS["AT"]:
            self.receiveError(" ".join(msg), "Number of parameters for AT is incorrect.")
            return

        origServer = msg[1]
        if origServer not in conf.PORT_NUM:
            self.receiveError(" ".join(msg), "Invalid erver name.")
            return

        client = msg[3]
        clientTime = msg[-1]
        if not validFloat(clientTime):
            self.receiveError(" ".join(msg), "Invalid time in the input.")
            return

        infoToStore = " ".join(msg[3:])
        origATMsg = " ".join(msg)
        msg[1] = self.server
        ATMsg = " ".join(msg)

        if client not in self.clients:
            self.clients[client] = infoToStore
            self.fp.write("Received Propogation:\n" + origATMsg + "\n")
            self.propagate(ATMsg, origServer, False)  # Propagate

        elif self.clients[client] != infoToStore:
            storeTime = self.clients[client].split()[-1]
            if float(storeTime) < float(clientTime):
                self.clients[client] = infoToStore
                self.fp.write("Received Propogation:\n" + " ".join(msg) + "\n")
                self.propagate(ATMsg, origServer, False)  # Propagate

    def handleWHATSAT(self, msg):
        if len(msg) != LENGTH_OR_ARGS["WHATSAT"]:
            self.receiveError(" ".join(msg), "Wrong number of parameters for WHATSAT.")
            return

        client = msg[1]
        radius = msg[2]
        upperBound = msg[3]
        if not validFloat(radius) or not upperBound.isdigit():
            self.receiveError(" ".join(msg), "Radius or Upper bound format incorrect for WHATSAT.")
            return
        radius = float(radius)
        upperBound = int(upperBound)
        if radius <= 0 or radius > 50 or upperBound <= 0 or upperBound > 20:
            self.receiveError(" ".join(msg), "Radius or Upper bound incorrect for WHATSAT.")
            return

        if client not in self.clients:
            self.receiveError(" ".join(msg), "Client ID not found.")
            return

        infoStored = self.clients[client].split()
        location = self.parseLocation(infoStored[1])
        radius *= 1000  # Change Unit

        url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?" \
              "location={0},{1}&radius={2}&key={3}".format(location[0], location[1], radius, conf.API_KEY)

        d = getPage(url).addCallback(self.handleJSON, upperBound = upperBound, client = client)
        d.addErrback(self.handleGoogleError, msg = msg)

    def handleJSON(self, data, upperBound, client):
        placeData = json.loads(data)
        placeData["results"] = placeData["results"][: upperBound]
        JSONStr = json.dumps(placeData, indent = 4, separators = (',', ': '))

        infoStored = self.clients[client]
        clientTime = infoStored.split()[-1]
        response = self.genATMsg(clientTime, infoStored) + "\n" + JSONStr
        response = response.rstrip("\n") + "\n\n"
        self.transport.write(response)
        self.fp.write("Server responds:\n" + response + "\n")

    def handleGoogleError(self, error, msg):
        self.receiveError(" ".join(msg), error)


class ServerFactory(Factory):
    numConnections = 0

    def __init__(self, serverName):
        self.clients = {}
        self.name = serverName
        self.file = serverName + "Log.txt"

    def buildProtocol(self, addr):
        return Server(self)

    def startFactory(self):
        self.fp = open(self.file, "a")
        self.fp.write("Create Log file for {0}\n".format(self.name))

    def stopFactory(self):
        self.fp.close()


def main():
    if len(sys.argv) != 2:
        sys.stderr.write("Wrong argument number {0}. Should be 2.\n".format(len(sys.argv)))
        exit(1)
    serverName = sys.argv[1]
    if serverName not in conf.PORT_NUM:
        sys.stderr.write("Invalid Server Name " + serverName + ".\n")
        exit(1)

    portNum = conf.PORT_NUM[serverName]
    reactor.listenTCP(portNum, ServerFactory(serverName))
    reactor.run()

if __name__ == "__main__":
    main()
