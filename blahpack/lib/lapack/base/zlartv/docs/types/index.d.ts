

// TypeScript declarations for @stdlib/lapack/base/zlartv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a vector of complex plane rotations with real cosines to two complex vectors.
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
* Apply a vector of complex plane rotations with real cosines to two complex vectors.
*/
declare var zlartv: Routine;

export = zlartv;
