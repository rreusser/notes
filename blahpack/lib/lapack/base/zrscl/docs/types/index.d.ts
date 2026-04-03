

// TypeScript declarations for @stdlib/lapack/base/zrscl

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Multiplies a complex vector by the reciprocal of a complex scalar.
	*/
	(
		N: number,
		a: any,
		x: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Multiplies a complex vector by the reciprocal of a complex scalar.
*/
declare var zrscl: Routine;

export = zrscl;
