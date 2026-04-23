'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaebz = require( './dlaebz.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaebz, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaebz;
