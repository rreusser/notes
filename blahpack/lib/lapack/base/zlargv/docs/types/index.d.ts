

// TypeScript declarations for @stdlib/lapack/base/zlargv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generates a vector of complex plane rotations with real cosines and complex sines.
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
		offsetC: number
	): Float64Array;
}

/**
* Generates a vector of complex plane rotations with real cosines and complex sines.
*/
declare var zlargv: Routine;

export = zlargv;
