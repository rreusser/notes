'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlagtm = require( './dlagtm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlagtm, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlagtm;
