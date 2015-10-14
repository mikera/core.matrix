package clojure.core.matrix.random;

import java.util.Random;

import clojure.lang.ASeq;
import clojure.lang.IPersistentCollection;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;
import clojure.lang.Obj;

/**
 * A Clojure sequence implementation that implements a sequence of random numbers
 * 
 * @author Mike
 *
 */
public class RandomSeq extends ASeq {
	private static final long serialVersionUID = -2985198079440712866L;
	private static final int INITIAL_CHUNK_SIZE=20;
	private static final int MAX_CHUNK_SIZE=160;
	
	final double[] data;
	Object rand;
	
	private RandomSeq(double[] data, Random random) {
		this.data=data;
		this.rand=random;
	}
	
	public RandomSeq(Random random) {
		this(produce(random,INITIAL_CHUNK_SIZE),random);
	}
	
	public static double[] produce(Random random, int count) {
		double[] data=new double[count];
		for (int i=0; i<count; i++) {
			data[i]=random.nextDouble();
		}
		return data;
	}
	
	@Override
	public int count() {
		throw new UnsupportedOperationException("RandomSeq has infinite elements");
	}

	@Override
	public IPersistentCollection empty() {
		return null;
	}

	@Override
	public Object first() {
		return data[0];
	}
	
	private synchronized RandomSeq nextChunk() {
		if (rand instanceof RandomSeq) return (RandomSeq)rand;
		
		Random random=(Random)rand;
		
		RandomSeq result= new RandomSeq(produce(random,Math.min(MAX_CHUNK_SIZE,data.length*2)),random);
		rand=result;
		return result;
	}

	@Override
	public ISeq next() {
		return new Cursor(1);
	}
	
	@Override
	public Obj withMeta(IPersistentMap meta) {
		throw new UnsupportedOperationException("RandomSeq does not support metadata");
	}
	
	private class Cursor extends ASeq {
		private static final long serialVersionUID = -4397476315573447323L;
		private final int pos;
		private final double[] data;
		
		private Cursor(double[] data, int pos) {
			this.pos=pos;
			this.data=data;
		}
		
		public Cursor(int pos) {
			this(RandomSeq.this.data,pos);
		}
		
		@Override
		public int count() {
			throw new UnsupportedOperationException("RandomSeq has infinite elements");
		}

		@Override
		public IPersistentCollection empty() {
			return null;
		}

		@Override
		public Object first() {
			return data[pos];
		}

		@Override
		public ISeq next() {
			int npos=pos+1;
			if (npos<data.length) return new Cursor(npos);
			return nextChunk();
		}

		@Override
		public Obj withMeta(IPersistentMap meta) {
			throw new UnsupportedOperationException("RandomSeq does not support metadata");
		}
	}

}
