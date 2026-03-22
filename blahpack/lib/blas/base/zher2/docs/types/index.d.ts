

// TypeScript declarations for @stdlib/blas/base/zher2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform Hermitian rank-2 update
	*/
	(
		uplo: string,
		N: number,
		alpha: any,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Perform Hermitian rank-2 update
*/
declare var zher2: Routine;

export = zher2;
