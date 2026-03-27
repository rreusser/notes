

// TypeScript declarations for @stdlib/blas/base/zsyr2k

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform one of the symmetric rank-2k operations C := alpha*A*B**T + alpha*B*A**T + beta*C or C := alpha*A**T*B + alpha*B**T*A + beta*C.
	*/
	(
		uplo: string,
		trans: string,
		N: number,
		K: number,
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
* Perform one of the symmetric rank-2k operations C := alpha*A*B**T + alpha*B*A**T + beta*C or C := alpha*A**T*B + alpha*B**T*A + beta*C.
*/
declare var zsyr2k: Routine;

export = zsyr2k;
