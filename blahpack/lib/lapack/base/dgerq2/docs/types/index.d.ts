

// TypeScript declarations for @stdlib/lapack/base/dgerq2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the RQ factorization of a real matrix (unblocked)
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
* Compute the RQ factorization of a real matrix (unblocked)
*/
declare var dgerq2: Routine;

export = dgerq2;
