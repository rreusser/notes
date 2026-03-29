

// TypeScript declarations for @stdlib/lapack/base/dlar2v

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a vector of real plane rotations from both sides to a sequence of 2-by-2 symmetric matrices.
	*/
	(
		N: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		z: Float64Array,
		strideZ: number,
		offsetZ: number,
		c: Float64Array,
		strideC: number,
		offsetC: number,
		s: Float64Array,
		strideS: number,
		offsetS: number
	): Float64Array;
}

/**
* Apply a vector of real plane rotations from both sides to a sequence of 2-by-2 symmetric matrices.
*/
declare var dlar2v: Routine;

export = dlar2v;
