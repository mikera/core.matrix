package clojure.core.matrix;

// A lightweight typed binary tuple for pairs of Class instances.
// This is designed to speed up binary multimethod dispatch, and does
// so by over 2x in an initial benchmark.
//
// Just a proof of concept for now; to be complete, this would need to
// implement IPersistentVector too to play nice with clojure.core/isa?

@SuppressWarnings("rawtypes")
public final class ClassPair {
	private final Class xClass, yClass;

	public static ClassPair fromObjects(Object x, Object y) {
		return new ClassPair(x.getClass(), y.getClass());
	}

	public ClassPair(Class xClass, Class yClass) {
		this.xClass = xClass;
		this.yClass = yClass;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof ClassPair) {
			final ClassPair o = (ClassPair) other;
			return o.xClass == xClass && o.yClass == yClass;
		}
		return false;
	}

	@Override
	public int hashCode() {
		return xClass.hashCode() + 37 * yClass.hashCode();
	}
}
