

// TypeScript declarations for @stdlib/lapack/base/zlapmt

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Rearrange columns of a complex matrix as specified by a permutation vector
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
* Rearrange columns of a complex matrix as specified by a permutation vector
*/
declare var zlapmt: Routine;

export = zlapmt;
