

// TypeScript declarations for @stdlib/blas/base/zgeru

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform complex rank-1 update (unconjugated)
	*/
	(
		M: number,
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
* Perform complex rank-1 update (unconjugated)
*/
declare var zgeru: Routine;

export = zgeru;
