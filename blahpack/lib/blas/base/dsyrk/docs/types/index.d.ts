

// TypeScript declarations for @stdlib/blas/base/dsyrk

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform symmetric rank-k update.
	*/
	(
		uplo: string,
		trans: string,
		N: number,
		K: number,
		alpha: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		beta: number,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number
	): Float64Array;
}

/**
* Perform symmetric rank-k update.
*/
declare var dsyrk: Routine;

export = dsyrk;
