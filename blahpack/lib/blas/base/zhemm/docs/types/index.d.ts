

// TypeScript declarations for @stdlib/blas/base/zhemm

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Performs Hermitian matrix-matrix multiplication
	*/
	(
		side: string,
		uplo: string,
		M: number,
		N: number,
		alpha: any,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		beta: any,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number
	): Float64Array;
}

/**
* Performs Hermitian matrix-matrix multiplication
*/
declare var zhemm: Routine;

export = zhemm;
