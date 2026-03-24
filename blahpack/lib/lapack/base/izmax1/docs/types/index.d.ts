

// TypeScript declarations for @stdlib/lapack/base/izmax1

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Find index of first element of maximum absolute value
	*/
	(
		N: number,
		x: Float64Array,
		stride: number,
		offset: number,
		incx: number
	): Float64Array;
}

/**
* Find index of first element of maximum absolute value
*/
declare var izmax1: Routine;

export = izmax1;
