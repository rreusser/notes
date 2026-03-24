

// TypeScript declarations for @stdlib/lapack/base/zsymv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex symmetric matrix-vector multiply
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
* Complex symmetric matrix-vector multiply
*/
declare var zsymv: Routine;

export = zsymv;
