

// TypeScript declarations for @stdlib/lapack/base/dlapmt

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Permute columns of a matrix
	*/
	(
		forwrd: boolean,
		M: number,
		N: number,
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number,
		k: Int32Array,
		strideK: number,
		offsetK: number
	): Float64Array;
}

/**
* Permute columns of a matrix
*/
declare var dlapmt: Routine;

export = dlapmt;
