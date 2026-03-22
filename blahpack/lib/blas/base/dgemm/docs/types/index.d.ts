

// TypeScript declarations for @stdlib/blas/base/dgemm

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Double-precision real matrix-matrix multiply.
	*/
	(
		transa: string,
		transb: string,
		M: number,
		N: number,
		K: number,
		alpha: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		beta: number,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number
	): Float64Array;
}

/**
* Double-precision real matrix-matrix multiply.
*/
declare var dgemm: Routine;

export = dgemm;
