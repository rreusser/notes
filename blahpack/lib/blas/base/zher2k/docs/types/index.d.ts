

// TypeScript declarations for @stdlib/blas/base/zher2k

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform Hermitian rank-2k update
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
		beta: number,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number
	): Float64Array;
}

/**
* Perform Hermitian rank-2k update
*/
declare var zher2k: Routine;

export = zher2k;
