

// TypeScript declarations for @stdlib/blas/base/dtrmm

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform one of the matrix-matrix operations B := alpha*op(A)*B or B := alpha*B*op(A) where A is a triangular matrix.
	*/
	(
		side: string,
		uplo: string,
		transa: string,
		diag: string,
		M: number,
		N: number,
		alpha: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Perform one of the matrix-matrix operations B := alpha*op(A)*B or B := alpha*B*op(A) where A is a triangular matrix.
*/
declare var dtrmm: Routine;

export = dtrmm;
