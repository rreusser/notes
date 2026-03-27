

// TypeScript declarations for @stdlib/lapack/base/dlartv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a vector of real plane rotations to two vectors.
	*/
	(
		N: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		c: Float64Array,
		strideC: number,
		offsetC: number,
		s: Float64Array,
		strideS: number,
		offsetS: number
	): Float64Array;
}

/**
* Apply a vector of real plane rotations to two vectors.
*/
declare var dlartv: Routine;

export = dlartv;
