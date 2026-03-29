

// TypeScript declarations for @stdlib/lapack/base/zlar2v

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a vector of complex plane rotations with real cosines from both sides to a sequence of 2-by-2 complex Hermitian matrices.
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
* Apply a vector of complex plane rotations with real cosines from both sides to a sequence of 2-by-2 complex Hermitian matrices.
*/
declare var zlar2v: Routine;

export = zlar2v;
