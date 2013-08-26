package clojure.core.matrix;

import com.google.caliper.Runner;
import com.google.caliper.SimpleBenchmark;



/**
 * Caliper based benchmarks
 *
 * @author Mike
 */

public class TestBenchmark extends SimpleBenchmark {

	public void timeVectorAddition(int runs) {
		// nothing here yet
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		new TestBenchmark().run();
	}

	private void run() {
		Runner runner=new Runner();
		runner.run(new String[] {this.getClass().getCanonicalName()});
	}

}
