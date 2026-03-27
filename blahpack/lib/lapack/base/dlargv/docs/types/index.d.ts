

// TypeScript declarations for @stdlib/lapack/base/dlargv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate a vector of real plane rotations.
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
* Generate a vector of real plane rotations.
*/
declare var dlargv: Routine;

export = dlargv;
