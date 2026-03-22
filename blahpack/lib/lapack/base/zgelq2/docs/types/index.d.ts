

// TypeScript declarations for @stdlib/lapack/base/zgelq2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute LQ factorization of a complex matrix (unblocked)
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
* Compute LQ factorization of a complex matrix (unblocked)
*/
declare var zgelq2: Routine;

export = zgelq2;
