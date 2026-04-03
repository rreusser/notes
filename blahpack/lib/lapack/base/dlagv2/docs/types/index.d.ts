

// TypeScript declarations for @stdlib/lapack/base/dlagv2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the Generalized Schur factorization of a real 2-by-2 matrix pencil (A,B) where B is upper triangular.
	*/
	(
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		alphar: Float64Array,
		strideALPHAR: number,
		offsetALPHAR: number,
		alphai: Float64Array,
		strideALPHAI: number,
		offsetALPHAI: number,
		beta: Float64Array,
		strideBETA: number,
		offsetBETA: number,
		csl: number,
		snl: number,
		csr: number,
		snr: number
	): Float64Array;
}

/**
* Computes the Generalized Schur factorization of a real 2-by-2 matrix pencil (A,B) where B is upper triangular.
*/
declare var dlagv2: Routine;

export = dlagv2;
