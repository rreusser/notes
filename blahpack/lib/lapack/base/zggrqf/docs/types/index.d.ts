

// TypeScript declarations for @stdlib/lapack/base/zggrqf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes a generalized RQ factorization of an M-by-N complex matrix A and a P-by-N complex matrix B.
	*/
	(
		M: number,
		p: number,
		N: number,
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
* Computes a generalized RQ factorization of an M-by-N complex matrix A and a P-by-N complex matrix B.
*/
declare var zggrqf: Routine;

export = zggrqf;
