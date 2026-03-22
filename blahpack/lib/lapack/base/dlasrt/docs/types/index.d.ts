

// TypeScript declarations for @stdlib/lapack/base/dlasrt

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Sort an array of doubles in increasing or decreasing order
	*/
	(
		id: string,
		N: number,
		d: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Sort an array of doubles in increasing or decreasing order
*/
declare var dlasrt: Routine;

export = dlasrt;
