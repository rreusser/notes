

// TypeScript declarations for @stdlib/blas/base/dger

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform the rank-1 update A := alpha*x*y**T + A.
	*/
	(
		M: number,
		N: number,
		alpha: number,
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
* Perform the rank-1 update A := alpha*x*y**T + A.
*/
declare var dger: Routine;

export = dger;
