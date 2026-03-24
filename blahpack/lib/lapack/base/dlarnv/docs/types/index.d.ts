

// TypeScript declarations for @stdlib/lapack/base/dlarnv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generates a vector of random numbers from a specified distribution
	*/
	(
		idist: number,
		iseed: Int32Array,
		strideISEED: number,
		offsetISEED: number,
		N: number,
		x: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Generates a vector of random numbers from a specified distribution
*/
declare var dlarnv: Routine;

export = dlarnv;
