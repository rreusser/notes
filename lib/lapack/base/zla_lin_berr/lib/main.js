'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaLinBerr = require( './zla_lin_berr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaLinBerr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaLinBerr;
