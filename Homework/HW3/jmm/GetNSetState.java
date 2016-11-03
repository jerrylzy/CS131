import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State
{
	//  public methods / interface
    
    GetNSetState(byte[] v)
    { 
    	copyToFieldArray(v);
    	maxval = 127;
    }

    GetNSetState(byte[] v, byte m) 
    { 
    	copyToFieldArray(v);
    	maxval = m;
    }

    public int size() { return value.length(); }

    public byte[] current() 
    { 
    	byte[] v = new byte[size()];
    	for (int i = 0; i < size(); i++)
    		v[i] = (byte) value.get(i);
    	return v;
    }

    public boolean swap(int i, int j) 
    {
    	int ival = value.get(i);
    	int jval = value.get(j);
		if (ival <= 0 || jval >= maxval)
		{
			//System.err.format("value of i: %d, value of j: %d", ival, jval);
		    return false;
		}

		value.set(i, ival - 1);
		value.set(j, jval + 1);
		return true;
    }
    
    //  private fields & methods

    private AtomicIntegerArray value;
    private byte maxval;
    private void copyToFieldArray(byte[] v)
    {
    	value = new AtomicIntegerArray(v.length);
    	for (int i = 0; i < v.length; i++)
    		value.set(i, v[i]);
    }
}
