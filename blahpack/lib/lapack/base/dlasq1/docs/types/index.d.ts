

// TypeScript declarations for @stdlib/lapack/base/dlasq1

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute all singular values of a real bidiagonal matrix via dqds
	*/
	(
		N: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Compute all singular values of a real bidiagonal matrix via dqds
*/
declare var dlasq1: Routine;

export = dlasq1;
