

// TypeScript declarations for @stdlib/lapack/base/zgerq2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex unblocked RQ factorization
	*/
	(
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Complex unblocked RQ factorization
*/
declare var zgerq2: Routine;

export = zgerq2;
