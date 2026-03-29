'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsyrfs = require( './dsyrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyrfs;
