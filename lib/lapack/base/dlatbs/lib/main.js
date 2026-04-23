'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlatbs = require( './dlatbs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlatbs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlatbs;
