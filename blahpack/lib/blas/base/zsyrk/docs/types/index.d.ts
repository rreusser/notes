

// TypeScript declarations for @stdlib/blas/base/zsyrk

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform one of the symmetric rank-k operations C := alpha*A*A**T + beta*C or C := alpha*A**T*A + beta*C.
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
		beta: any,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number
	): Float64Array;
}

/**
* Perform one of the symmetric rank-k operations C := alpha*A*A**T + beta*C or C := alpha*A**T*A + beta*C.
*/
declare var zsyrk: Routine;

export = zsyrk;
