'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqge = require( './zlaqge.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqge, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqge;
