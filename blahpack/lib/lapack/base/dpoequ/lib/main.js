'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpoequ = require( './dpoequ.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpoequ, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpoequ;
