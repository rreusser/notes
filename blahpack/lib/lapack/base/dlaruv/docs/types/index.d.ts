

// TypeScript declarations for @stdlib/lapack/base/dlaruv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generates a vector of random numbers from a uniform distribution
	*/
	(
		iseed: Int32Array,
		strideISEED: number,
		offsetISEED: number,
		N: number,
		x: Float64Array,
		strideX: number,
		offsetX: number
	): Float64Array;
}

/**
* Generates a vector of random numbers from a uniform distribution
*/
declare var dlaruv: Routine;

export = dlaruv;
