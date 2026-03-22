

// TypeScript declarations for @stdlib/lapack/base/dlasq5

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute one dqds transform with shift
	*/
	(
		i0: number,
		n0: number,
		z: Float64Array,
		stride: number,
		offset: number,
		pp: number,
		tau: number,
		sigma: number,
		dmin: number,
		dmin1: number,
		dmin2: number,
		dn: number,
		dnm1: number,
		dnm2: number,
		ieee: boolean,
		eps: number
	): Float64Array;
}

/**
* Compute one dqds transform with shift
*/
declare var dlasq5: Routine;

export = dlasq5;
