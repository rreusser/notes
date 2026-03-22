

// TypeScript declarations for @stdlib/lapack/base/dorgl2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate an orthogonal matrix from an LQ factorization (unblocked)
	*/
	(
		M: number,
		N: number,
		K: number,
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
* Generate an orthogonal matrix from an LQ factorization (unblocked)
*/
declare var dorgl2: Routine;

export = dorgl2;
