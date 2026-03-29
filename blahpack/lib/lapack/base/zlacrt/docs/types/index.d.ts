

// TypeScript declarations for @stdlib/lapack/base/zlacrt

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a plane rotation with complex cosine and sine to two complex vectors.
	*/
	(
		N: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		incx: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		incy: number,
		c: any,
		s: any
	): Float64Array;
}

/**
* Apply a plane rotation with complex cosine and sine to two complex vectors.
*/
declare var zlacrt: Routine;

export = zlacrt;
