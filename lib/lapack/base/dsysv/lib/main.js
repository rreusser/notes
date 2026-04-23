'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsysv = require( './dsysv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsysv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsysv;
