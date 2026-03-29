'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsycon = require( './dsycon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsycon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsycon;
