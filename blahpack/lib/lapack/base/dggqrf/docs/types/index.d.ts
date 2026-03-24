

// TypeScript declarations for @stdlib/lapack/base/dggqrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes a generalized QR factorization of matrices A and B
	*/
	(
		N: number,
		M: number,
		p: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		TAUA: Float64Array,
		strideTAUA: number,
		offsetTAUA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		TAUB: Float64Array,
		strideTAUB: number,
		offsetTAUB: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Computes a generalized QR factorization of matrices A and B
*/
declare var dggqrf: Routine;

export = dggqrf;
