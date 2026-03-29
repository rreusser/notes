'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zherfs = require( './zherfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zherfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zherfs;
