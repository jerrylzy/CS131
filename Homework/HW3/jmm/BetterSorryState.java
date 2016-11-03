import java.util.concurrent.atomic.AtomicIntegerArray;

class BetterSorryState implements State
{
	//  public methods / interface
    
    BetterSorryState(byte[] v)
    { 
    	copyToFieldArray(v);
    	maxval = 127;
    }

    BetterSorryState(byte[] v, byte m) 
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
		if (value.get(i) <= 0 || value.get(j) >= maxval)
		    return false;

		value.getAndDecrement(i);
		value.getAndIncrement(j);
		return true;
    }
    
    //  private fields & methods

    private AtomicIntegerArray value;
    int ival;
    int jval;
    private byte maxval;
    private void copyToFieldArray(byte[] v)
    {
    	value = new AtomicIntegerArray(v.length);
    	for (int i = 0; i < v.length; i++)
    		value.set(i, v[i]);
    }
}
