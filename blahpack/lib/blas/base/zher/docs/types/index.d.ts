

// TypeScript declarations for @stdlib/blas/base/zher

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform Hermitian rank-1 update
	*/
	(
		uplo: string,
		N: number,
		alpha: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Perform Hermitian rank-1 update
*/
declare var zher: Routine;

export = zher;
