

// TypeScript declarations for @stdlib/lapack/base/dgelq2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the LQ factorization of a real matrix (unblocked)
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
* Compute the LQ factorization of a real matrix (unblocked)
*/
declare var dgelq2: Routine;

export = dgelq2;
