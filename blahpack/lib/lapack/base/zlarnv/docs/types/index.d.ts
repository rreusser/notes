

// TypeScript declarations for @stdlib/lapack/base/zlarnv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns a vector of complex random numbers from a uniform or normal distribution.
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
* Returns a vector of complex random numbers from a uniform or normal distribution.
*/
declare var zlarnv: Routine;

export = zlarnv;
