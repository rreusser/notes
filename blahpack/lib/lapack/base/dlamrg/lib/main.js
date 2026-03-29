'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlamrg = require( './dlamrg.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlamrg, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlamrg;
