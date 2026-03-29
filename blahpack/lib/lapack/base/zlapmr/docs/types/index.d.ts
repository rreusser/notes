

// TypeScript declarations for @stdlib/lapack/base/zlapmr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Rearrange the rows of a complex matrix as specified by a permutation vector.
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
* Rearrange the rows of a complex matrix as specified by a permutation vector.
*/
declare var zlapmr: Routine;

export = zlapmr;
