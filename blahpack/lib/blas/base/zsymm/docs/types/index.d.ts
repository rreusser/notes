

// TypeScript declarations for @stdlib/blas/base/zsymm

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform one of the symmetric matrix-matrix operations C := alpha*A*B + beta*C or C := alpha*B*A + beta*C.
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
* Perform one of the symmetric matrix-matrix operations C := alpha*A*B + beta*C or C := alpha*B*A + beta*C.
*/
declare var zsymm: Routine;

export = zsymm;
