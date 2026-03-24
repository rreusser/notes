

// TypeScript declarations for @stdlib/blas/base/drotmg

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Construct a modified Givens plane rotation
	*/
	(
		dd1: number,
		dd2: number,
		dx1: number,
		dy1: number,
		dparam: Float64Array,
		strideDPARAM: number,
		offsetDPARAM: number
	): Float64Array;
}

/**
* Construct a modified Givens plane rotation
*/
declare var drotmg: Routine;

export = drotmg;
