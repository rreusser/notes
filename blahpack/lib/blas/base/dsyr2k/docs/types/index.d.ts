

// TypeScript declarations for @stdlib/blas/base/dsyr2k

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform symmetric rank-2k update
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
* Perform symmetric rank-2k update
*/
declare var dsyr2k: Routine;

export = dsyr2k;
