

// TypeScript declarations for @stdlib/lapack/base/dlamrg

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* LAPACK dlamrg routine
	*/
	(
		n1: number,
		n2: number,
		a: Float64Array,
		strideA: number,
		offsetA: number,
		dtrd1: number,
		dtrd2: number,
		INDEX: Int32Array,
		strideINDEX: number,
		offsetINDEX: number
	): Float64Array;
}

/**
* LAPACK dlamrg routine
*/
declare var dlamrg: Routine;

export = dlamrg;
