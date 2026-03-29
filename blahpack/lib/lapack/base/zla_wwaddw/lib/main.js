'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_wwaddw = require( './zla_wwaddw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_wwaddw, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_wwaddw;
