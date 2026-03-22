

// TypeScript declarations for @stdlib/lapack/base/dlassq

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Return an updated sum of squares represented in scaled form
	*/
	(
		N: number,
		x: Float64Array,
		stride: number,
		offset: number,
		scale: number,
		sumsq: number
	): Float64Array;
}

/**
* Return an updated sum of squares represented in scaled form
*/
declare var dlassq: Routine;

export = dlassq;
