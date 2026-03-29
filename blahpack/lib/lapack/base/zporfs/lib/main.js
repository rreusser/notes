'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zporfs = require( './zporfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zporfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zporfs;
