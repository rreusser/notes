

// TypeScript declarations for @stdlib/lapack/base/dla_wwaddw

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Add a vector into a doubled-single accumulation vector.
	*/
	(
		N: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		w: Float64Array,
		strideW: number,
		offsetW: number
	): Float64Array;
}

/**
* Add a vector into a doubled-single accumulation vector.
*/
declare var dla_wwaddw: Routine;

export = dla_wwaddw;
