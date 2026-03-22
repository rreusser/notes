

// TypeScript declarations for @stdlib/blas/base/zhemv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform Hermitian matrix-vector multiplication
	*/
	(
		uplo: string,
		N: number,
		alpha: any,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		beta: any,
		y: Float64Array,
		strideY: number,
		offsetY: number
	): Float64Array;
}

/**
* Perform Hermitian matrix-vector multiplication
*/
declare var zhemv: Routine;

export = zhemv;
