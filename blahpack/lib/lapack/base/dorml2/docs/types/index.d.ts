

// TypeScript declarations for @stdlib/lapack/base/dorml2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Multiply a general matrix by the orthogonal matrix from an LQ factorization (unblocked)
	*/
	(
		side: string,
		trans: string,
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
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Multiply a general matrix by the orthogonal matrix from an LQ factorization (unblocked)
*/
declare var dorml2: Routine;

export = dorml2;
