

// TypeScript declarations for @stdlib/blas/base/dsymm

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Performs symmetric matrix-matrix multiplication
	*/
	(
		side: string,
		uplo: string,
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
		offsetB: number,
		beta: number,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number
	): Float64Array;
}

/**
* Performs symmetric matrix-matrix multiplication
*/
declare var dsymm: Routine;

export = dsymm;
